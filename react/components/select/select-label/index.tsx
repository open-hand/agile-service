import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { issueLabelApi } from '@/api';
import { ILabel } from '@/common/types';
import { MAX_LENGTH_LABEL } from '@/constants/MAX_LENGTH';

export interface SelectLabelProps extends Partial<SelectProps> {
  dataRef?: React.RefObject<Array<any>>
  valueField?: string
  afterLoad?: (sprints: ILabel[]) => void
  flat?: boolean
  projectId?: string
  extraOptions?: any[]
}

const SelectLabel: React.FC<SelectLabelProps> = forwardRef(({
  dataRef, valueField, afterLoad, flat, projectId, extraOptions = [], ...otherProps
}, ref: React.Ref<Select>) => {
  const { combo } = otherProps;
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
    combo: combo ?? true,
  }), [combo]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;

  return (
    <Component
      ref={ref}
      multiple
      placeholder="输入即可创建新标签"
      maxLength={combo ? MAX_LENGTH_LABEL : undefined}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectLabel;
