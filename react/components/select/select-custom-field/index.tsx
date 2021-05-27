import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { fieldApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { unionBy, castArray, partition } from 'lodash';
import { usePersistFn } from 'ahooks';

interface BasicProps extends Partial<SelectProps> {

  selected?: string[]
  extraOptions?: any[]
  flat?: boolean
  projectId?: string
  organizationId?: string
  outside?: boolean

}
// 参数互斥，要么传fieldId，要么传fieldOptions
type SelectCustomFieldProps = BasicProps & ({
  fieldId: string
  fieldOptions?: never
} | {
  fieldId?: never
  // 把所有options传进来，前端假分页
  fieldOptions: {
    id: string
    value: string
  }[]
})

const SelectCustomField: React.FC<SelectCustomFieldProps> = forwardRef(({
  fieldId, fieldOptions, flat, projectId, organizationId, selected, extraOptions, outside = false, ...otherProps
},
ref: React.Ref<Select>) => {
  const needOptions = useMemo(() => [...castArray(otherProps.value), ...castArray(selected)].filter(Boolean), [selected, otherProps.value]);
  const fakePageRequest = usePersistFn((filter: string = '', page: number = 1, size: number, ensureOptions: string[]) => {
    if (!fieldOptions) {
      return [];
    }
    const [ensuredOptions, restOptions] = partition(fieldOptions, (item) => ensureOptions.includes(item.id));
    const list = restOptions.filter((item) => item.value && item.value?.indexOf(filter) > -1).slice(Math.max((page - 1) * size, 0), (page) * size);
    return {
      // 第一页包含已选的选项
      list: page === 1 ? [...ensuredOptions, ...list] : list,
      hasNextPage: restOptions.length > (page) * size,
    };
  });
  const config = useMemo((): SelectConfig => ({
    textField: 'value',
    valueField: 'id',
    request: ({ page, filter }) => (fieldOptions ? fakePageRequest(filter, page, 10, needOptions) : fieldApi.outside(outside).org(organizationId).project(projectId).getFieldOptions(fieldId!, filter, page, 10, needOptions)),
    middleWare: (data) => {
      if (!extraOptions) {
        return data;
      }
      return unionBy([...extraOptions, ...data], 'id');
    },
    paging: true,
  }), [fieldOptions, fakePageRequest, needOptions, outside, organizationId, projectId, fieldId, extraOptions]);
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
