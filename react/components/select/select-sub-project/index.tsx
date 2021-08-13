import React, { useMemo, forwardRef } from 'react';
import { Select, Tooltip } from 'choerodon-ui/pro';

import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { commonApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { IIssueType } from '@/common/types';

export interface SelectSubProjectProps extends Partial<SelectProps> {
  dataRef?: React.MutableRefObject<any>
  hasUnassign?: boolean
  afterLoad?: (types: IIssueType[]) => void
}

const SelectSubProject: React.FC<SelectSubProjectProps> = forwardRef(({
  dataRef, afterLoad, hasUnassign, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig<IIssueType> => ({
    name: 'subProject',
    textField: 'projName',
    valueField: 'projectId',
    request: () => commonApi.getSubProjects(true).then((res: any) => (Array.isArray(res) ? res.map((i) => ({ ...i, projectId: String(i.projectId) })) : [])),
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
      optionRenderer={({ record, text, value }) => (
        <Tooltip title={text}>
          <span>{text}</span>
        </Tooltip>
      )}
      {...otherProps}

    />
  );
});
export default SelectSubProject;
