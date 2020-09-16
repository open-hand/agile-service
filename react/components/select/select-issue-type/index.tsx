import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { Tooltip } from 'choerodon-ui';
import { issueTypeApi, sprintApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IIssueType, ISprint } from '@/common/types';

interface Props extends Partial<SelectProps> {
  filterList?: string[]
  afterLoad?: (sprints: IIssueType[]) => void
}

const SelectIssueType: React.FC<Props> = forwardRef(({
  filterList = ['feature'],
  afterLoad, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<IIssueType> => ({
    name: 'sprint',
    textField: 'name',
    valueField: 'id',
    request: () => issueTypeApi.loadAllWithStateMachineId(),
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
