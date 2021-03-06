# Infer_parallel

 This library tries to contain the core parallel, message-passing, and
 task bar components used in
 [Infer](https://github.com/facebook/infer/tree/v1.1.0). It wraps them
 in a tidier interface, and builds with `dune`. It is a personal best
 effort and offered 'as-is': there is no guarantee for maintenance.

 This project was inspired by Rijnard van Tonder's
 [hack_parallel](https://github.com/rvantonder/hack_parallel).

## LIMITATIONS
<!-- only support map, iter -->
<!-- only in a single multi-core machine -->
<!-- infer v1.1.0 -->
<!-- no guarantee for maintenance -->
<!-- Since it uses marshalling for communication, messages MUST not be bigger than [buffer_size] after marshaled which is under 64kb.  -->


## Samples
 You can find a sample code in [sample/](sample/).

### Multiline
![](resources/infer_parallel_sample_multiline.gif)

### Multiline + Keep Going
![](resources/infer_parallel_sample_keep_going.gif)

### Multiline + Abort (NOT Keep Going)
![](resources/infer_parallel_sample_abort.gif)

### Quiet
![](resources/infer_parallel_sample_quiet.gif)
