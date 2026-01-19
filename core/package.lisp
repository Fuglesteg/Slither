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
           #:entity-encode
           #:entity-decode
           #:entity-find-behavior
           #:entity-behaviors
           #:entity-type-id
           #:entity-find-networked-slot-id
           #:entity-networked-slots-with-behaviors
           #:entity-networked-slots
           #:*behavior*
           #:behavior
           #:behavior-invoke
           #:defbehavior
           #:behavior-required-behaviors
           #:behavior-entity
           #:with-behaviors
           #:current-tick))
