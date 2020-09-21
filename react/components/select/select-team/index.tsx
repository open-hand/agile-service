import React, { useMemo, forwardRef, useRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { commonApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';

interface Props extends Partial<SelectProps> {
  projectDataRef?: React.RefObject<Array<any>>,
  afterLoad?: (projects: any[]) => void
}

const SelectTeam: React.FC<Props> = forwardRef(({ projectDataRef = { current: null }, afterLoad, ...otherProps }, ref: React.Ref<Select>) => {
  const afterLoadRef = useRef<Function>();
  afterLoadRef.current = afterLoad;
  const config = useMemo((): SelectConfig => ({
    name: 'team',
    textField: 'projName',
    valueField: 'projectId',
    request: () => commonApi.getSubProjects(true),
    paging: false,
    middleWare: (projects) => {
      // @ts-ignore
      // eslint-disable-next-line
        projectDataRef.current = projects;
      if (afterLoadRef.current) {
        afterLoadRef.current(projects);
      }
      return projects;
    },
  }), []);
  const props = useSelect(config);
  return (
    <Select
      ref={ref}
      {...props}
      {...otherProps}
    />
  );
});
export default SelectTeam;
