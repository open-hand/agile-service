import React, { useMemo, forwardRef, useRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { quickFilterApi } from '@/api';
import type { PI } from '@/common/types';

interface Props extends Partial<SelectProps> {
    afterLoad?: (piList: PI[]) => void
    disabledCurrentPI?: boolean
}
interface Filter {
    filterId: string,
    name: string,
}
const QuickFilterField: React.FC<Props> = forwardRef(({
  disabledCurrentPI = false, afterLoad, ...otherProps
}, ref: React.Ref<Select>) => {
  const afterLoadRef = useRef<Function>();
  afterLoadRef.current = afterLoad;
  const config = useMemo((): SelectConfig<Filter> => ({
    name: 'quick',
    textField: 'name',
    valueField: 'filterId',
    request: () => quickFilterApi.loadAll({ contents: [], filterName: '' }),
    middleWare: (list) => {
      if (afterLoadRef.current) {
        afterLoadRef.current(list);
      }
      return list;
    },
    paging: false,
  }), []);
  const props = useSelect(config);
  return (
    <Select
      ref={ref}
      {...props}
      {...otherProps}
    />
  );
});
export default QuickFilterField;
