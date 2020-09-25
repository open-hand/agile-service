import React, { useMemo, forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { epicApi } from '@/api';
import useSelect, { SelectConfig } from '@/hooks/useSelect';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { IEpic } from '@/components/charts/epic-report/search';

interface Props extends Partial<SelectProps> {
  isProgram?: boolean
  dataRef?: React.MutableRefObject<any>
  afterLoad?: (epics: IEpic[]) => void
}

const SelectEpic: React.FC<Props> = forwardRef(({
  isProgram, afterLoad, dataRef, ...otherProps
}, ref: React.Ref<Select>) => {
  const config = useMemo((): SelectConfig => ({
    name: 'epic',
    textField: 'epicName',
    valueField: 'issueId',
    request: () => (isProgram ? epicApi.loadProgramEpics() : epicApi.loadEpics()),
    middleWare: (epicList:IEpic[]) => {
      if (isProgram) {
        epicList.unshift({ issueId: '0', epicName: '未分配史诗' });
      }
      if (dataRef) {
        Object.assign(dataRef, {
          current: isProgram ? epicList.unshift({ issueId: '0', epicName: '未分配史诗' }) : epicList,
        });
      }
      if (afterLoad) {
        afterLoad(epicList);
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
