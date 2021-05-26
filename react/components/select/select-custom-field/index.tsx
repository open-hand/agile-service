import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { fieldApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { unionBy } from 'lodash';

interface Props extends Partial<SelectProps> {
  fieldId: string
  selected?: string[]
  extraOptions?: any[]
  flat?: boolean
  projectId?: string
}

const SelectCustomField: React.FC<Props> = forwardRef(({
  fieldId, flat, projectId, selected, extraOptions, ...otherProps
},
ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    textField: 'value',
    valueField: 'id',
    request: ({ page, filter }) => fieldApi.project(projectId).getFieldOptions(fieldId, filter, page, 10, selected),
    middleWare: (data) => {
      if (!extraOptions) {
        return data;
      }
      return unionBy([...extraOptions, ...data], 'id');
    },
    paging: true,
  }), [extraOptions, fieldId, projectId, selected]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;
  return (
    <Component
      ref={ref}
      {...props}
      {...otherProps}
      // @ts-ignore
      textField={null}
      valueField={null}
    />
  );
});
export default SelectCustomField;
