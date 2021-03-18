import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { Tooltip } from 'choerodon-ui';
import { find } from 'lodash';
import { commonApi, issueTypeApi, sprintApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IIssueType, ISprint } from '@/common/types';

interface Props extends Partial<SelectProps> {
  dataRef?: React.MutableRefObject<any>
  hasUnassign?: boolean
  afterLoad?: (types: IIssueType[]) => void
}

const SelectSubProject: React.FC<Props> = forwardRef(({
  dataRef, afterLoad, hasUnassign, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<IIssueType> => ({
    name: 'subProject',
    textField: 'projName',
    valueField: 'projectId',
    request: () => commonApi.getSubProjects(true),
    middleWare: (data) => {
      let newData = data;
      if (dataRef) {
        Object.assign(dataRef, {
          current: newData,
        });
      }
      if (afterLoad) {
        afterLoad(newData);
      }
      if (hasUnassign) {
        newData = [{ projectId: '0', projName: '未分配团队' } as unknown as IIssueType, ...newData];
      }
      return newData;
    },
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
