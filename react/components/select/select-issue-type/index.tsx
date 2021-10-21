import React, { useMemo, forwardRef } from 'react';
import { Select, Tooltip } from 'choerodon-ui/pro';

import { find, includes } from 'lodash';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import { issueTypeApi, sprintApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { IIssueType, ISprint } from '@/common/types';
import TypeTag from '@/components/TypeTag';

export interface SelectIssueTypeProps extends Partial<SelectProps> {
  filterList?: string[]
  request?: SelectConfig<any>['request']
  isProgram?: boolean
  afterLoad?: (sprints: IIssueType[]) => void
  dataRef?: React.MutableRefObject<any>
  valueField?: string
  flat?: boolean
  projectId?: string
  applyType?: string
  excludeTypeCodes?: string[]
  excludeTypeIds?: string[]
  showIcon?: boolean

}

const SelectIssueType: React.FC<SelectIssueTypeProps> = forwardRef(({
  filterList = ['feature'], isProgram, request, valueField, dataRef, flat, excludeTypeIds = [], showIcon,
  afterLoad, projectId, applyType, excludeTypeCodes, ...otherProps
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
    paging: false,
  }), [afterLoad, applyType, dataRef, excludeTypeCodes, excludeTypeIds, filterList, isProgram, projectId, request, showIcon, valueField]);
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
