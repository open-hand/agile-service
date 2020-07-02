import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { commonApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';

interface Props extends SelectProps {
  projectDataRef?: React.RefObject<Array<any>>
}

const SelectTeam: React.FC<Props> = forwardRef(({ projectDataRef, ...otherProps }, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    name: 'team',
    textField: 'projName',
    valueField: 'projectId',
    request: () => commonApi.getSubProjects(true),
    paging: false,
    middleWare: (projects) => {
      if (projectDataRef) {
        // @ts-ignore
        // eslint-disable-next-line
        projectDataRef.current = projects;
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
