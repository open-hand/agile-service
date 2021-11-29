import React, { createContext, useState, useContext } from 'react';
import { inject } from 'mobx-react';
import useIsInProgram from '@/hooks/useIsInProgram';
import { getMenuType, getApplyType } from '@/utils/common';

import { disabledEditDefaultFields } from '../page-issue-type/utils';

interface Context {
  objectDetailItem: { schemeCode: string },
  isInProgram: boolean

}
const PageConfigContext = createContext({} as Context);
export function usePageConfigContext() {
  return useContext(PageConfigContext);
}
const PageConfigProvider = inject('AppState')(
  (props: any) => {
    const [reLoad, setReLoad] = useState(false);
    const [objectDetailItem, setObjectDetailItem] = useState({
      schemeCode: 'agile_issue',
    });
    const { isInProgram, loading } = useIsInProgram();
    if (isInProgram) {
      disabledEditDefaultFields.push('epic');
    } else if (getApplyType() === 'program') {
      disabledEditDefaultFields.push('sprint');
    } else {
      const newDisabledEditDefaultFields = disabledEditDefaultFields.filter((item) => !['epic', 'sprint'].includes(item));
      if (newDisabledEditDefaultFields.length !== disabledEditDefaultFields.length) {
        disabledEditDefaultFields.length = 0;
        disabledEditDefaultFields.push(...newDisabledEditDefaultFields);
      }
    }
    const value = {
      ...props,
      objectDetailItem,
      setObjectDetailItem,
      reLoad,
      setReLoad,
      isInProgram,
    };
    return (
      <PageConfigContext.Provider value={value}>
        {!loading && props.children}
      </PageConfigContext.Provider>
    );
  },
);
export default PageConfigProvider;
