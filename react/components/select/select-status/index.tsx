import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { IStatusCirculation, statusTransformApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import FlatSelect from '@/components/flat-select';

interface Props extends Partial<SelectProps> {
  issueTypeId?: string
  expectStatusId?: string
  dataRef?: React.MutableRefObject<any>
  afterLoad?: (statuss: IStatusCirculation[]) => void
  request?: Function
  flat?: boolean
  projectId?: string
  applyType?: 'program' | 'agile'
}

const SelectStatus: React.FC<Props> = forwardRef(
  ({
    request, issueTypeId, expectStatusId, dataRef, afterLoad, flat, projectId, applyType, ...otherProps
  }, ref: React.Ref<Select>) => {
    const config = useMemo((): SelectConfig<IStatusCirculation> => ({
      name: 'status',
      textField: 'name',
      valueField: 'id',
      request: async () => {
        if (issueTypeId) {
          return statusTransformApi.project(projectId).loadList(issueTypeId, applyType);
        }
        if (request) {
          return request();
        }
        return Promise.resolve([]);
      },
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
    }), [afterLoad, dataRef, expectStatusId, issueTypeId, request]);
    const props = useSelect(config);
    const Component = flat ? FlatSelect : Select;
    return (
      <Component
        ref={ref}
        {...props}
        {...otherProps}
      />
    );
  },
);
export default SelectStatus;
