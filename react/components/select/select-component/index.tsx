import React, { useMemo, forwardRef } from 'react';
import { Select, Tooltip } from 'choerodon-ui/pro';
import { componentApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IComponent } from '@/common/types';
import { FlatSelect } from '@choerodon/components';
import { find, toArray, uniqBy } from 'lodash';

interface Props extends Partial<SelectProps> {
  dataRef?: React.MutableRefObject<any>
  valueField?: string
  afterLoad?: (components: IComponent[]) => void
  flat?: boolean
  projectId?: string
  selected?: IComponent[]
}

const SelectComponent: React.FC<Props> = forwardRef(({
  dataRef, afterLoad, valueField, flat, projectId, selected, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<IComponent> => ({
    name: 'component',
    textField: 'name',
    valueField: valueField || 'componentId',
    request: ({ page, filter }) => componentApi.loadAllComponents(filter, projectId, page, 10),
    middleWare: (components) => {
      // @ts-ignore
      let data = components || [];
      if (dataRef) {
        Object.assign(dataRef, {
          current: data,
        });
      }
      if (selected) {
        selected.forEach((item) => {
          data.push(item);
        });
      }
      data = uniqBy(data, 'componentId');
      if (afterLoad) {
        afterLoad(data);
      }
      return data;
    },
    paging: true,
    tooltip: true,
  }), [dataRef, projectId, selected, valueField, flat]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;

  return (
    <Component
      ref={ref}
      clearButton
      multiple
      maxTagTextLength={10}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectComponent;
