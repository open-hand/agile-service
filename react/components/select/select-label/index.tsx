import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { issueLabelApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { ILabel } from '@/common/types';
import FlatSelect from '@/components/flat-select';

interface Props extends Partial<SelectProps> {
  dataRef?: React.RefObject<Array<any>>
  valueField?: string
  afterLoad?: (sprints: ILabel[]) => void
  flat?:boolean
  projectId?: string
  extraOptions?: any[]
}

const SelectLabel: React.FC<Props> = forwardRef(({
  dataRef, valueField, afterLoad, flat, projectId, extraOptions = [], ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    name: 'label',
    textField: 'labelName',
    valueField: valueField || 'labelId',
    request: () => issueLabelApi.loads(projectId),
    middleWare: (data: ILabel[]) => {
      if (dataRef) {
        Object.assign(dataRef, {
          current: [...extraOptions, ...data],
        });
      }
      if (afterLoad) {
        afterLoad([...extraOptions, ...data]);
      }
      return [...extraOptions, ...data];
    },
    paging: false,
  }), []);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;

  return (
    <Component
      ref={ref}
      multiple
      combo
      {...props}
      {...otherProps}
    />
  );
});
export default SelectLabel;
