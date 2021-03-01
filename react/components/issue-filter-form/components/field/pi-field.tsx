import React, { useMemo, forwardRef, useRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import useSelect, { SelectConfig, FragmentForSearch } from '@/hooks/useSelect';

import { piApi } from '@/api';
import type { PI } from '@/common/types';

interface Props extends Partial<SelectProps> {
  afterLoad?: (piList: PI[]) => void
  disabledCurrentPI?: boolean
}
const PIField: React.FC<Props> = forwardRef(({
  disabledCurrentPI = false, afterLoad, ...otherProps
}, ref: React.Ref<Select>) => {
  const afterLoadRef = useRef<Function>();
  afterLoadRef.current = afterLoad;
  const config = useMemo((): SelectConfig<PI> => ({
    name: 'all_pi',
    textField: 'piName',
    valueField: 'id',
    request: () => piApi.getPiListByStatus(),
    optionRenderer: (pi) => (
      <FragmentForSearch name={`${pi.code}-${pi.name}`}>
        {pi.code === 'none' ? pi.name : pi.fullName || `${pi.code}-${pi.name}`}
      </FragmentForSearch>
    ),
    middleWare: (piList) => {
      if (afterLoadRef.current) {
        afterLoadRef.current(piList);
      }
      const newPiList = [{
        id: 'none',
        name: '未分配PI',
        piName: '未分配PI',
        code: 'none',
        startDate: '',
        endDate: '',
        statusCode: '',
        actualStartDate: null,
        actualEndDate: null,
      }, ...piList];
      return newPiList as any;
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
export default PIField;
