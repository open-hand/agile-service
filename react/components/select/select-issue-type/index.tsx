import React, { useMemo, forwardRef } from 'react';
import { Select, Tooltip } from 'choerodon-ui/pro';

import { castArray, find, includes } from 'lodash';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { useCreation } from 'ahooks';
import { issueTypeApi, sprintApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { IIssueType, ISprint } from '@/common/types';
import TypeTag from '@/components/TypeTag';

export interface SelectIssueTypeProps extends Partial<SelectProps> {
  filterList?: string[]
  request?: SelectConfig<any>['request']
  level?: 'workbench'
  isProgram?: boolean
  afterLoad?: (sprints: IIssueType[]) => void
  dataRef?: React.MutableRefObject<any>
  valueField?: string
  defaultSelectedIds?: string[]

  flat?: boolean
  projectId?: string
  applyType?: string
  excludeTypeCodes?: string[]
  excludeTypeIds?: string[]
  showIcon?: boolean
  onlyEnabled?: boolean
}

const SelectIssueType: React.FC<SelectIssueTypeProps> = forwardRef(({
  filterList = ['feature'], isProgram, request, valueField, dataRef, flat, excludeTypeIds = [], showIcon, defaultSelectedIds: propsDefaultSelectedIds,
  afterLoad, projectId, applyType, excludeTypeCodes, onlyEnabled = false, level, ...otherProps
}, ref: React.Ref<Select>) => {
  const defaultSelectedIds = useCreation(() => castArray(propsDefaultSelectedIds).filter(Boolean), []);

  const config = useMemo((): SelectConfig<IIssueType> => ({
    name: 'issueType',
    textField: 'name',
    valueField: valueField || 'id',
    request: request || (({ filter: filterWord, page, requestArgs }) => {
      if (level === 'workbench') {
        return issueTypeApi.loadPageForWorkbench({ page, param: filterWord, size: 50 },
          { filterIssueTypeIds: defaultSelectedIds });
      }
      return issueTypeApi.loadAllWithStateMachineId(applyType ?? (isProgram ? 'program' : undefined), projectId).then((issueTypes) => {
        if (isProgram) {
          const featureTypes: any = [{
            id: 'business',
            name: '特性',
            colour: '',
            description: '',
            icon: '',
            stateMachineId: '',
            typeCode: '',
          }, {
            id: 'enabler',
            name: '使能',
            colour: '',
            description: '',
            icon: '',
            stateMachineId: '',
            typeCode: '',
          }];
          const epicType: IIssueType = find<IIssueType>(issueTypes, { typeCode: 'issue_epic' }) as IIssueType;
          return [...featureTypes, epicType];
        }
        if (Array.isArray(filterList) && filterList.length > 0) {
          return issueTypes.filter((issueType) => (onlyEnabled ? issueType.enabled : true && !filterList.some((filter) => filter === issueType.typeCode)));
        }
        return issueTypes;
      });
    }),
    optionRenderer: showIcon ? (issueType) => <TypeTag data={issueType} showName /> : undefined,
    middleWare: (issueTypes) => {
      const newData = issueTypes.filter((item) => !(includes(excludeTypeCodes, item.typeCode) || includes(excludeTypeIds, item.id)));
      if (afterLoad) {
        afterLoad(newData);
      }
      if (dataRef) {
        Object.assign(dataRef, {
          current: newData,
        });
      }
      return newData;
    },
    paging: level === 'workbench',
  }), [afterLoad, applyType, dataRef, excludeTypeCodes, excludeTypeIds, filterList, isProgram, onlyEnabled, projectId, request, showIcon, valueField]);
  const props = useSelect(config);
  const Component = flat ? FlatSelect : Select;

  return (
    <Component
      ref={ref}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectIssueType;
