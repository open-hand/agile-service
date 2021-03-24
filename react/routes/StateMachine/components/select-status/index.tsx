import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { issueTypeApi, statusApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IStatus } from '@/common/types';

interface Props extends Partial<SelectProps> {
  expectStatusIds?: string[]
  isOrganization?: boolean
}

const SelectStatus: React.FC<Props> = forwardRef(({
  expectStatusIds,
  isOrganization = false,
  ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<IStatus> => ({
    name: 'status',
    textField: 'name',
    valueField: 'id',
    request: ({ page, filter }) => (isOrganization ? statusApi.loadList(page, 20, '', {
      name: filter,
    }) : statusApi.loadByProject()),
    middleWare: (res: IStatus[] | { list: IStatus[]}) => {
      const statusList: IStatus[] = (Array.isArray(res) ? res : res.list) || [];
      return (expectStatusIds && expectStatusIds.length > 0
        ? statusList.filter((status) => !expectStatusIds.includes(status.id))
        : statusList);
    },
    paging: !!isOrganization,
  }), [expectStatusIds, isOrganization]);
  const props = useSelect(config);
  return (
    <Select
      ref={ref}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectStatus;
