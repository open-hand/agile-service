import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { stores } from '@choerodon/boot';
import { IStatusCirculation, statusTransformApi, issueTypeApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IIssueType } from '@/common/types';

const { AppState } = stores;
interface Props extends Partial<SelectProps> {
  issueTypeCode?: string,
  projectId?: string
  applyType?: 'program' | 'agile'
  issueTypeId?: string
  expectStatusId?: string
  dataRef?: React.MutableRefObject<any>
  afterLoad?: (statuss: IStatusCirculation[]) => void
}

const SelectStatus: React.FC<Props> = forwardRef(
  ({
    projectId, applyType, issueTypeId, issueTypeCode, expectStatusId, dataRef, afterLoad, ...otherProps
  }, ref: React.Ref<Select>) => {
    const config = useMemo((): SelectConfig<IStatusCirculation> => ({
      name: 'status',
      textField: 'name',
      valueField: 'id',
      request: async () => {
        if (issueTypeId) {
          return statusTransformApi.loadList(issueTypeId);
        } if (issueTypeCode && projectId) {
          const type = AppState.currentMenuType.category === 'PROGRAM' ? 'program' : 'agile';
          const issueTypes: IIssueType[] = await issueTypeApi.loadAllWithStateMachineId(applyType || type, projectId) || [];
          const typeId = issueTypes.find((item) => item.typeCode === issueTypeCode)?.id;
          if (typeId) {
            return statusTransformApi.project(projectId).loadList(typeId, applyType || type);
          }
          return Promise.resolve([]);
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
    }), [afterLoad, applyType, dataRef, expectStatusId, issueTypeCode, issueTypeId, projectId]);
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
