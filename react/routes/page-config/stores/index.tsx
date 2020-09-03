import React, { createContext, useState, useContext } from 'react';
import { inject } from 'mobx-react';
import { injectIntl } from 'react-intl';
import IsProgramContext from '@/hooks/useIsProgrom';
import { getApplyType } from '@/utils/common';

interface Context {
  objectDetailItem: { schemeCode: string },

}
const PageConfigContext = createContext({} as Context);
export function usePageConfigContext() {
  return useContext(PageConfigContext);
}
const PageConfigProvider = injectIntl(inject('AppState')(
  (props:any) => {
    const [reLoad, setReLoad] = useState(false);
    const [objectDetailItem, setObjectDetailItem] = useState({
      schemeCode: 'agile_issue',
    });
    const value = {
      ...props,
      objectDetailItem,
      setObjectDetailItem,
      reLoad,
      setReLoad,
    };
    return (
      <IsProgramContext.Provider value={{ isProgram: getApplyType() === 'program' }}>
        <PageConfigContext.Provider value={value}>
          {props.children}
        </PageConfigContext.Provider>
      </IsProgramContext.Provider>
    );
  },
));
export default PageConfigProvider;
