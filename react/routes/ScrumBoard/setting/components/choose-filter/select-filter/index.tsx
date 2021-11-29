import React, { forwardRef, useMemo } from 'react';
import { FlatSelect } from '@choerodon/components';
import { Select, Tooltip } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { omit } from 'lodash';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { boardApi } from '@/api';
import useFormatMessage from '@/hooks/useFormatMessage';

const { Option } = FlatSelect;

interface IFilter {
  filterId: string,
  name: string,
  expressQuery: string
}

export interface SelectFilterProps extends Partial<SelectProps> {
  dataRef?: React.RefObject<Array<any>>
  valueField?: string
  afterLoad?: (filters: IFilter[]) => void
  flat?: boolean
  projectId?: string
  boardId: string
  defaultSelectedIds?: string[]
}

const SelectFilter: React.FC<SelectFilterProps> = forwardRef(({
  boardId, dataRef, afterLoad, flat, defaultSelectedIds: propsDefaultSelectedIds, ...otherProps
}, ref: React.Ref<Select>) => {
  const formatMessage = useFormatMessage('agile.scrumBoard');
  const args = useMemo(() => ({ selectIds: propsDefaultSelectedIds }), [propsDefaultSelectedIds]);

  const config = useMemo((): SelectConfig => ({
    name: 'filter',
    textField: 'name',
    valueField: 'filterId',
    requestArgs: args,
    request: ({ page, filter, requestArgs }) => boardApi.getQuickFilterList(boardId, { filterName: filter || '', contents: [], quickFilterIds: requestArgs?.selectIds }, page),
    middleWare: (data: IFilter[]) => {
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
    paging: true,
    optionRenderer: (filter: IFilter) => (
      <Tooltip title={filter.expressQuery}>
        <span>{filter.name}</span>
      </Tooltip>
    ),
  }), [afterLoad, args, boardId, dataRef]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;

  return (
    <Component
      ref={ref}
      // @ts-ignore
      label={formatMessage({ id: 'choose.quick.filter' })}
      multiple
      {...props}
      {...otherProps}
    />
  );
});
export default SelectFilter;
