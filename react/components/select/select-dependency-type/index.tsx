import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { FlatSelect } from '@choerodon/components';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { ganttApi } from '@/api';
import { DependencyItem, transformDependencyData } from '@/utils/transformDependencyData';

interface Props extends Partial<SelectProps> {
  flat?: boolean
  issueId?: string
}

const SelectDependencyType: React.FC<Props> = forwardRef(({
  flat, issueId, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<DependencyItem> => ({
    textField: 'name',
    valueField: 'valueCode',
    request: () => (ganttApi.loadIssueDependencyTypes()),
    middleWare: (data) => transformDependencyData(data),
    paging: false,
  }), [issueId]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;

  return (
    <Component
      ref={ref}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectDependencyType;
