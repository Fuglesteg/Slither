(uiop:define-package #:slither/core
  (:use #:cl
        #:org.shirakumo.fraf.math.vectors
        #:org.shirakumo.fraf.math.matrices
        #:slither/utils)
  (:export #:start
           #:tick
           #:*entity*
           #:entity
           #:entity-invoke
           #:defentity
           #:entity-find-behavior
           #:entity-behaviors
           #:*behavior*
           #:behavior
           #:behavior-invoke
           #:defbehavior
           #:behavior-required-behaviors
           #:behavior-entity
           #:with-behaviors))
