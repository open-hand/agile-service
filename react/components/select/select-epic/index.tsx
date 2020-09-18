import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { epicApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';

interface Props {
  isProgram?: boolean
}

const SelectEpic: React.FC<Props> = forwardRef(({ isProgram, ...otherProps }, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    name: 'epic',
    textField: 'epicName',
    valueField: 'issueId',
    request: () => (isProgram ? epicApi.loadProgramEpics() : epicApi.loadEpics()),
    middleWare: (epicList) => {
      if (isProgram) {
        epicList.unshift({ issueId: '0', epicName: '未分配史诗' });
      }
      return epicList;
    },
    paging: false,
  }), []);
  const props = useSelect(config);
  return (
    <Select
      ref={ref}
      clearButton
      {...props}
      {...otherProps}
    />
  );
});
export default SelectEpic;
