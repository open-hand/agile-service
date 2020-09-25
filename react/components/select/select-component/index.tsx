import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { componentApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IComponent } from '@/common/types';

interface Props extends Partial<SelectProps> {
  dataRef?: React.MutableRefObject<any>
  valueField?: string
  afterLoad?: (components: IComponent[]) => void
}

const SelectComponent: React.FC<Props> = forwardRef(({
  dataRef, afterLoad, valueField, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    name: 'component',
    textField: 'name',
    valueField: valueField || 'name',
    request: () => componentApi.loadAllComponents(),
    middleWare: (components) => {
      // @ts-ignore
      const data = components.content || [];
      if (dataRef) {
        Object.assign(dataRef, {
          current: data,
        });
      }
      if (afterLoad) {
        afterLoad(data);
      }
      return data;
    },
    paging: false,
  }), []);
  const props = useSelect(config);
  return (
    <Select
      ref={ref}
      clearButton
      multiple
      combo
      {...props}
      {...otherProps}
    />
  );
});
export default SelectComponent;
