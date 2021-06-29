import React, { useMemo } from 'react';
import { Select } from 'choerodon-ui/pro';
import { statusApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IStatus } from '@/common/types';

interface Props extends Partial<SelectProps> {
  issueTypeId?: string
  isProgram?: boolean,
  dataRef?: React.MutableRefObject<any>,
  afterLoad?: (statusList: IStatus[]) => void
}
const SelectStatus: React.FC<Props> = ({
  issueTypeId, dataRef, isProgram, afterLoad, ...otherProps
}) => {
  const config = useMemo((): SelectConfig<IStatus> => ({
    name: 'statusId',
    textField: 'name',
    valueField: 'id',
    request: ({ filter, page }) => statusApi.loadByProject(isProgram ? 'program' : 'agile'),
    middleWare: (data) => {
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
      {...props}
      {...otherProps}
    />
  );
};
export default SelectStatus;
