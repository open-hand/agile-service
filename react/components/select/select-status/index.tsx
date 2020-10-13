import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { IStatusCirculation, statusTransformApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IStatus } from '@/common/types';

interface Props extends Partial<SelectProps> {
  issueTypeId?: string
  expectStatusId?: string
  dataRef?: React.MutableRefObject<any>
  afterLoad?: (statuss: IStatusCirculation[]) => void
}

const SelectStatus: React.FC<Props> = forwardRef(
  ({
    issueTypeId, expectStatusId, dataRef, afterLoad, ...otherProps
  }, ref: React.Ref<Select>) => {
    const config = useMemo((): SelectConfig<IStatusCirculation> => ({
      name: 'status',
      textField: 'name',
      valueField: 'id',
      request: () => (issueTypeId ? statusTransformApi.loadList(issueTypeId) : Promise.resolve([])),
      middleWare: (statusList) => {
        const data = expectStatusId
          ? statusList.filter(({ id }) => id !== expectStatusId)
          : statusList;
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
    }), [issueTypeId]);
    const props = useSelect(config);
    return (
      <Select
        ref={ref}
        {...props}
        {...otherProps}
      />
    );
  },
);
export default SelectStatus;
