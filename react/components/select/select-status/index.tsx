import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { castArray, includes, intersection } from 'lodash';
import { useCreation } from 'ahooks';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { IStatusCirculation, statusApi, statusTransformApi } from '@/api';

export interface SelectStatusProps extends Partial<SelectProps> {
  issueTypeId?: string
  /** 无工作项类型查询状态 用以全查状态 */
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
  defaultSelectedIds?: string[]
  isOrganization?: boolean
  isProgram?: boolean
  isBacklog?: boolean
  isWorkBench?: boolean
  extraStatus?: IStatusCirculation[]
}

const SelectStatus: React.FC<SelectStatusProps> = forwardRef(
  ({
    request, issueTypeId, noIssueTypeIdQuery, excludeStatus = [], defaultSelectedIds: propsDefaultSelectedIds, isProgram, isBacklog, isWorkBench, dataRef, afterLoad, flat, projectId, applyType: propsApplyType, issueTypeIds, selectedIds, isOrganization = false, extraStatus = [], ...otherProps
  }, ref: React.Ref<Select>) => {
    const defaultSelectedIds = useCreation(() => castArray(propsDefaultSelectedIds).filter(Boolean), []);
    let applyType = isProgram ? 'program' : 'agile';

    if (isBacklog) {
      applyType = 'backlog';
    }
    applyType = propsApplyType ?? applyType;
    const args = useMemo(() => ({ selectedIds: defaultSelectedIds }), [defaultSelectedIds]);
    const config = useMemo((): SelectConfig<IStatusCirculation> => ({
      name: 'status',
      textField: 'name',
      valueField: 'id',
      requestArgs: args,
      request: async ({ filter, page, requestArgs }) => {
        if (request) {
          return request();
        }
        if (isWorkBench) {
          return statusApi.loadAllFromWorkbench({ page, param: filter, size: 50 }, {
            ignoredStatusIds: requestArgs?.selectedIds,
            queryIgnored: true,
          });
        }
        if (noIssueTypeIdQuery) {
          return statusApi.project(projectId).loadByProject(applyType as any);
        }
        if (issueTypeId && (!applyType || ['program', 'backlog', 'agile'].includes(applyType))) {
          return isOrganization ? statusTransformApi.orgLoadList(issueTypeId) : statusTransformApi.project(projectId).loadList(issueTypeId, applyType as any);
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
      paging: isWorkBench || false,
    }), [issueTypeId, args, request, isOrganization, isWorkBench, projectId, applyType, excludeStatus, extraStatus, issueTypeIds, dataRef, afterLoad, selectedIds]);
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
SelectStatus.displayName = 'SelectStatus';
export default SelectStatus;
