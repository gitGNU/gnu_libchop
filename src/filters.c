#include <chop/chop.h>
#include <chop/serializable.h>
#include <chop/filters.h>

/* Define the `chop_filter_t' class.  */

static void
filter_ctor (chop_object_t *object, const chop_class_t *class)
{
  chop_filter_t *filter = (chop_filter_t *)object;

  filter->input_fault_handler.handle = NULL;
  filter->output_fault_handler.handle = NULL;
  filter->within_fault_handler = 0;
}

CHOP_DEFINE_RT_CLASS (filter, object,
		      filter_ctor, NULL,
		      NULL, NULL);
