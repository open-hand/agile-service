import React, { useMemo } from 'react';
import { Select } from 'choerodon-ui/pro';
import { loadEpics } from '@/api/NewIssueApi';
import useSelect, { SelectConfig } from '@/hooks/useSelect';

interface Props {
}

const SelectEpic: React.FC<Props> = (otherProps) => {
    const config = useMemo((): SelectConfig => ({
      name: 'epic',
      textField: 'epicName',
      valueField: 'issueId',
      request: () => loadEpics(),
      paging: false,
    }), []);
    const props = useSelect(config);
    return (
      <Select
        clearButton={true}
        {...props}
        {...otherProps}
      />
    );
  };
export default SelectEpic;
