import React, { useMemo, forwardRef } from 'react';
import { Select, Tooltip } from 'choerodon-ui/pro';

import { find, includes } from 'lodash';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { issueTypeApi, sprintApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { IIssueType, ISprint } from '@/common/types';

export interface SelectIssueTypeProps extends Partial<SelectProps> {
  filterList?: string[]
  request?: SelectConfig<any>['request']
  isProgram?: boolean
  afterLoad?: (sprints: IIssueType[]) => void
  dataRef?: React.MutableRefObject<any>
  valueField?: string
  flat?: boolean
  projectId?:string
  applyType?:string
  excludeTypeIds?: string[]
}

const SelectIssueType: React.FC<SelectIssueTypeProps> = forwardRef(({
  filterList = ['feature'], isProgram, request, valueField, dataRef, flat, excludeTypeIds = [],
  afterLoad, projectId, applyType, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<IIssueType> => ({
    name: 'issueType',
    textField: 'name',
    valueField: valueField || 'id',
    request: request || (() => issueTypeApi.loadAllWithStateMachineId(applyType ?? (isProgram ? 'program' : undefined), projectId).then((issueTypes) => {
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
        return issueTypes.filter((issueType) => !filterList.some((filter) => filter === issueType.typeCode));
      }
      return issueTypes;
    })),
    middleWare: (issueTypes) => {
      const newData = issueTypes.filter((item) => !includes(excludeTypeIds, item.id));
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
    paging: false,
  }), [afterLoad, applyType, dataRef, excludeTypeIds, filterList, isProgram, projectId, request, valueField]);
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
