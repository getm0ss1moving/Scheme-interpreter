
#ifndef WEAK_PTR
#define WEAK_PTR
#include <cstddef>
#include <iostream>

struct ControlBlock {
    size_t strong_count=0;
    size_t weak_count=0;
};
template <typename T>
class WeakPtr;
template <typename T>
class SharedPtr
{
public:
    SharedPtr():_p(nullptr),count(nullptr){};
    explicit SharedPtr(T* p)
        : _p(p) {
        if (_p) {
            count = new ControlBlock{1,0};
        }
        else{
            count = nullptr;
        }
    }
    SharedPtr(T *p,ControlBlock *_count) :_p(p),count(_count){
        if (count) {
            ++(count->strong_count);
        }
    };
    ~SharedPtr(){release();count = nullptr;_p = nullptr;};
    SharedPtr(const SharedPtr& other)
        : _p(other._p), count(other.count) {
        if (count) {
            ++(count->strong_count);
        }
    }
    
    SharedPtr& operator=(const SharedPtr& other) {
        if (this != &other) {
            release();
            _p = other._p;
            if(_p){
                count = other.count;
                ++(count->strong_count);
            }
            else{count = nullptr;}
        }
        return *this;
    }
    operator bool()const{return _p != nullptr;};
    size_t use_count() const{
        return count?count->strong_count:0;
    };
    T* get() const{return _p;};
    ControlBlock* get_control_block() const{return count;}
    T& operator* () const{return *_p;};
    T* operator->() const{return _p;};
    void reset(){release();count = nullptr;
        _p = nullptr;};
    void reset(T* p){
        release();
        _p=p;
        if(_p){
            count = new ControlBlock{1,0};
        }
        else{
            _p=nullptr;
            count = nullptr;
        }
    }
private:
     T* _p;
    ControlBlock* count;

    void release() {
        if (count!=nullptr) {
            if (--(count->strong_count) == 0) {
                delete _p;
            }
            if ((count->strong_count)==0&&(count->weak_count) == 0) {
                delete count;  
            }
        }
        
    }

    // Grant WeakPtr access
    friend class WeakPtr<T>;//it seems to be useless here
};
template <typename T, typename... Args>
SharedPtr<T> make_shared(Args&&... args) {
  return SharedPtr<T>(new T(std::forward<Args>(args)...));
}
// Include SharedPtr definition
// When submitting to ACMOJ, please replace this with the content inside shared_ptr.h

template <typename T>
class WeakPtr {
public:
    // Constructors
    WeakPtr():_p(nullptr),count(nullptr){};  // Default constructor
    WeakPtr(const WeakPtr& other):_p(other._p),count(other.count){
        if (count!=nullptr&&count->strong_count!=0) {
            ++(count->weak_count);
        }
        else{count=nullptr;}
    }  // Copy constructor

    WeakPtr(WeakPtr&& other) noexcept{
        _p=other._p;
        other._p = nullptr;
        count=other.count;
        other.count = nullptr;
    } // Move constructor
    WeakPtr(const SharedPtr<T>& other)
        : _p(other.get()) {
            
        if(other.count==nullptr||other.count->strong_count==0){
            count=nullptr;
            _p=nullptr;
        }
        else{
            const ControlBlock* tmp=other.get_control_block(); //avoid to use private var directly
            if(tmp){
                count = const_cast<ControlBlock*> (tmp);   
                ++(count->weak_count);
            }
        }
    }  // Construct from SharedPtr
    
    // Destructor
    ~WeakPtr(){release();_p=nullptr;count = nullptr;} // Destruct;
    
    // Assignment operators
    WeakPtr& operator=(const WeakPtr& other){
        if(this == &other){return *this;}
        else{
            release();
            _p=other._p;
            count=other.count;
            if(count->strong_count==0){count=nullptr;}
            if(count){
                ++(count->weak_count);
            }
        }
        return *this;
    }  // Copy assignment
    WeakPtr& operator=(WeakPtr&& other) noexcept{
        if (this != &other) {
            release();
            _p = other._p;
            count = other.count;
            other._p = nullptr;
            other.count = nullptr;
        }
        return *this;
    }  // Move assignment
    WeakPtr& operator=(const SharedPtr<T>& other){
        release();
        _p = other._p;
        if(other.count==nullptr||other.count->strong_count==0){
            count=nullptr;
            _p=nullptr;
        }
        else{
            const ControlBlock* tmp=other.get_control_block();
            if(tmp){
                count = const_cast<ControlBlock*> (tmp);
                ++(count->weak_count);
            }
        }
        return *this;
    }  // SharedPtr assignment
    
    // Basic operationsW
    void reset(){release();_p=nullptr;count = nullptr;};  // Reset to empty state
    size_t use_count() const{
        return count?count->strong_count:0;
    };  // Get the number of shared owners
    bool expired() const{
        if(count&&count->strong_count!=0){return false;}
        return true; 
    };  // Check if the managed object was deleted
    SharedPtr<T> lock()  {
        if (count && count->strong_count > 0) {
            return SharedPtr<T>(_p,count);
        }
        return SharedPtr<T>();
    }  // Get a SharedPtr to the managed object
    
    // Utility functions
    
    void swap(WeakPtr& other) noexcept{
        std::swap(_p, other._p);
        std::swap(count, other.count);
    }  // Swap with another WeakPtr

private:
     T* _p;
    ControlBlock* count;

    void release() {
        if (count) {
            if ( --(count->weak_count) == 0) {
                if(count->strong_count==0){
                    delete count;
                }
            }
        }
    }

    // Allow SharedPtr to construct from WeakPtr
    friend class SharedPtr<T>;
};

// Non-member swap function
template <typename T>
void swap(WeakPtr<T>& lhs, WeakPtr<T>& rhs) noexcept {
    lhs.swap(rhs);
}

#endif //WEAK_PTR