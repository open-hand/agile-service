import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { devOpsApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import FlatSelect from '@/components/flat-select';

export interface IService {
  id: string
  name: string
}
interface Props extends Partial<SelectProps> {
  projectId: string
  afterLoad?: (data: IService[]) => void
  flat?: boolean
}

const SelectService: React.FC<Props> = forwardRef(({
  flat, afterLoad, projectId, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<IService> => ({
    name: 'SelectService',
    textField: 'name',
    valueField: 'id',
    request: () => devOpsApi.project(projectId).loadActiveService(),
    afterLoad,
    paging: false,
  }), [afterLoad, projectId]);
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
export default SelectService;
