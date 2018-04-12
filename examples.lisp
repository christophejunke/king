
(let* ((root (make-instance 'sprite-node :name 'toto :parent nil :children (make-array 0 :adjustable t :fill-pointer 0)))
       (child (make-instance 'sprite-node :name 'child :parent root :children nil)))
  (vector-push-extend child (sprite-node-children root))
  (values root child))


(descend-leaf (spritesheet-tree (find-spritesheet 'kingdom::forest))
              '(kingdom::goldie kingdom::walk kingdom::forward kingdom::left))

(switch-to-sprite kingdom::*king-run-forward-left* 'kingdom::helmet)
(switch-to-sprite kingdom::*king-run-forward-left* '(kingdom::snake kingdom::crawl))

(switch-to-sprite kingdom::*king-run-forward-left* 'kingdom::idle)
