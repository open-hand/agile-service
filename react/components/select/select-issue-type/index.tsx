import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { Tooltip } from 'choerodon-ui';
import { find } from 'lodash';
import { issueTypeApi, sprintApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IIssueType, ISprint } from '@/common/types';

interface Props extends Partial<SelectProps> {
  filterList?: string[]
  isProgram?: boolean
  afterLoad?: (sprints: IIssueType[]) => void
}

const SelectIssueType: React.FC<Props> = forwardRef(({
  filterList = ['feature'], isProgram,
  afterLoad, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<IIssueType> => ({
    name: 'issueType',
    textField: 'name',
    valueField: 'id',
    request: () => issueTypeApi.loadAllWithStateMachineId(isProgram ? 'program' : undefined).then((issueTypes) => {
      if (isProgram) {
        const featureTypes = [{
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
    }),
    middleWare: (issueTypes) => {
      if (afterLoad) {
        afterLoad(issueTypes);
      }
      return issueTypes;
    },
    paging: false,
  }), []);
  const props = useSelect(config);
  return (
    <Select
      ref={ref}
      {...props}
      {...otherProps}
    // optionRenderer={({ record, text, value }) => (
    //   <Tooltip title={text}>
    //     <span>{text}</span>
    //   </Tooltip>
    // )}
    />
  );
});
export default SelectIssueType;
