import React, { useMemo, forwardRef } from 'react';
import { SelectBox } from 'choerodon-ui/pro';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { fieldApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { unionBy, castArray, partition } from 'lodash';
import { usePersistFn } from 'ahooks';

interface BasicProps extends Partial<SelectProps> {

  selected?: string[] | string
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
  onlyEnabled?: boolean
} | {
  fieldId?: never
  // 把所有options传进来，前端假分页
  fieldOptions: {
    id: string
    value: string
    enabled: boolean
  }[]
  onlyEnabled?: boolean
})
const SIZE = 0;
const SelectCustomField: React.FC<SelectCustomFieldProps> = forwardRef(({
  fieldId, fieldOptions, flat, projectId, organizationId, selected, extraOptions, outside = false, onlyEnabled = true, ...otherProps
},
ref: React.Ref<SelectBox>) => {
  const needOptions = useMemo(() => [...castArray(otherProps.value), ...castArray(selected)].filter(Boolean), [selected, otherProps.value]);
  const fakePageRequest = usePersistFn((filter: string = '', page: number = 1, size: number, ensureOptions: string[], enabled: boolean = true) => {
    if (!fieldOptions) {
      return [];
    }
    const [ensuredOptions, restOptions] = partition(fieldOptions, (item) => ensureOptions.includes(item.id));
    const list = restOptions.filter((item) => (enabled ? item.enabled : true)).filter((item) => item.value && item.value?.indexOf(filter) > -1).slice(Math.max((page - 1) * size, 0), (page) * size);
    return {
      // 第一页包含已选的选项
      list: page === 1 ? [...ensuredOptions, ...list] : list,
      hasNextPage: restOptions.length > (page) * size,
    };
  });
  const config = useMemo((): SelectConfig => ({
    textField: 'value',
    valueField: 'id',
    request: ({ page, filter }) => (fieldOptions ? fakePageRequest(filter, page, SIZE, needOptions, onlyEnabled) : fieldApi.outside(outside).org(organizationId).project(projectId).getFieldOptions(fieldId!, filter, page, SIZE, needOptions, onlyEnabled)),
    middleWare: (data) => {
      if (!extraOptions) {
        return data;
      }
      return unionBy([...extraOptions, ...data], 'id');
    },
    paging: true,
  }), [fieldOptions, fakePageRequest, needOptions, outside, organizationId, projectId, fieldId, extraOptions]);
  const props = useSelect(config);
  return (
    <SelectBox
      ref={ref}
      {...props}
      {...otherProps}
      // @ts-ignore
      textField={null}
      valueField={null}
      searchable={false}
    />
  );
});
export default SelectCustomField;
