import React, { createContext, useState, useContext } from 'react';
import { inject } from 'mobx-react';
import { injectIntl } from 'react-intl';
import useIsInProgram from '@/hooks/useIsInProgram';
import { disabledEditDefaultFields } from '../page-issue-type/utils';

interface Context {
  objectDetailItem: { schemeCode: string },
  isInProgram: boolean

}
const PageConfigContext = createContext({} as Context);
export function usePageConfigContext() {
  return useContext(PageConfigContext);
}
const PageConfigProvider = injectIntl(inject('AppState')(
  (props: any) => {
    const [reLoad, setReLoad] = useState(false);
    const [objectDetailItem, setObjectDetailItem] = useState({
      schemeCode: 'agile_issue',
    });
    const { isInProgram, loading } = useIsInProgram();
    if (isInProgram) {
      disabledEditDefaultFields.push('epic');
    } else {
      const newDisabledEditDefaultFields = disabledEditDefaultFields.filter((item) => item !== 'epic');
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
));
export default PageConfigProvider;
