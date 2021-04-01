import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import {
  issueTypeApi, IStatusCirculation, statusApi, statusTransformApi,
} from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IStatus } from '@/common/types';
import { includes, intersection } from 'lodash';

interface Props extends Partial<SelectProps> {
  issueTypeId?: string
  expectStatusId?: string
  isProgram?: boolean,
  dataRef?: React.MutableRefObject<any>,
  afterLoad?: (statusList: IStatus[]) => void
  issueTypeIds?: string[]
  selectedIds?: string[]
  isBacklog?: boolean
}
const SelectStatus: React.FC<Props> = ({
  issueTypeId, expectStatusId, dataRef, isProgram, afterLoad, issueTypeIds, selectedIds, isBacklog = false, ...otherProps
}) => {
  let applyType = 'agile';
  if (isProgram) {
    applyType = 'program';
  }
  if (isBacklog) {
    applyType = 'backlog';
  }
  const config = useMemo((): SelectConfig<IStatus> => ({
    name: 'statusId',
    textField: 'name',
    valueField: 'id',
    request: ({ filter, page }) => statusApi.loadByProject(applyType),
    middleWare: (statusList) => {
      let data = statusList;
      if (dataRef) {
        Object.assign(dataRef, {
          current: data,
        });
      }
      if (issueTypeIds) {
        data = data.filter((item) => {
          if (includes(selectedIds, item.id)) {
            return true;
          }
          // @ts-ignore
          if (item.issueTypeIds) {
            // @ts-ignore
            return intersection(issueTypeIds, item.issueTypeIds).length > 0;
          }
          return true;
        });
      }
      if (afterLoad) {
        afterLoad(data);
      }
      return data;
    },
    paging: false,
  }), [issueTypeIds, selectedIds]);
  const props = useSelect(config);
  return (
    <Select
      {...props}
      {...otherProps}
    />
  );
};
export default SelectStatus;
