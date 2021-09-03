import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { includes, intersection } from 'lodash';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { IStatusCirculation, statusApi, statusTransformApi } from '@/api';

export interface SelectStatusProps extends Partial<SelectProps> {
  issueTypeId?: string
  /** 无问题类型查询状态 用以全查状态 */
  noIssueTypeIdQuery?: boolean
  excludeStatus?: string[]
  dataRef?: React.MutableRefObject<any>
  afterLoad?: (statuss: IStatusCirculation[]) => void
  request?: Function
  flat?: boolean
  projectId?: string
  applyType?: 'program' | 'agile' | 'backlog'
  issueTypeIds?: string[]
  selectedIds?: string[]
  isOrganization?: boolean
  isProgram?: boolean
  isBacklog?: boolean
  extraStatus?: IStatusCirculation[]
}

const SelectStatus: React.FC<SelectStatusProps> = forwardRef(
  ({
    request, issueTypeId, noIssueTypeIdQuery, excludeStatus = [], isProgram, isBacklog, dataRef, afterLoad, flat, projectId, applyType: propsApplyType, issueTypeIds, selectedIds, isOrganization = false, extraStatus = [], ...otherProps
  }, ref: React.Ref<Select>) => {
    let applyType = propsApplyType;
    if (isProgram) {
      applyType = 'program';
    }
    if (isBacklog) {
      applyType = 'backlog';
    }
    const config = useMemo((): SelectConfig<IStatusCirculation> => ({
      name: 'status',
      textField: 'name',
      valueField: 'id',
      request: async () => {
        if (noIssueTypeIdQuery) {
          return statusApi.project(projectId).loadByProject(applyType as any);
        }
        if (issueTypeId && (!applyType || ['program', 'backlog'].includes(applyType))) {
          return isOrganization ? statusTransformApi.orgLoadList(issueTypeId) : statusTransformApi.project(projectId).loadList(issueTypeId, applyType as any);
        }
        if (request) {
          return request();
        }
        return Promise.resolve([]);
      },
      middleWare: (statusList) => {
        let data = excludeStatus?.length
          ? [...(statusList.filter(({ id }) => !excludeStatus.includes(id)) || []), ...extraStatus]
          : [...(statusList || []), ...extraStatus];
        if (issueTypeIds) {
          data = data.filter((item) => {
            if (includes(selectedIds, item.id)) {
              return true;
            }
            if (item.issueTypeIds) {
              return intersection(issueTypeIds, item.issueTypeIds).length > 0;
            }
            return true;
          });
        }
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
    }), [issueTypeId, request, isOrganization, projectId, applyType, excludeStatus, extraStatus, issueTypeIds, dataRef, afterLoad, selectedIds]);
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
