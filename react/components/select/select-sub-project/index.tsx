import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { Tooltip } from 'choerodon-ui';
import { find } from 'lodash';
import { commonApi, issueTypeApi, sprintApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IIssueType, ISprint } from '@/common/types';

interface Props extends Partial<SelectProps> {
}

const SelectSubProject: React.FC<Props> = forwardRef((otherProps, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<IIssueType> => ({
    name: 'subProject',
    textField: 'projName',
    valueField: 'projectId',
    request: () => commonApi.getSubProjects(true),
    paging: false,
  }), []);
  const props = useSelect(config);
  return (
    <Select
      ref={ref}
      {...props}
      {...otherProps}
      optionRenderer={({ record, text, value }) => (
        <Tooltip title={text}>
          <span>{text}</span>
        </Tooltip>
      )}
    />
  );
});
export default SelectSubProject;
