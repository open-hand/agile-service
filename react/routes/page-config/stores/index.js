import React, { createContext, useState } from 'react';
import { inject } from 'mobx-react';
import { injectIntl } from 'react-intl';

const PageConfigContext = createContext();
export default PageConfigContext;

export const PageConfigProvider = injectIntl(inject('AppState')(
  (props) => {
    const [reLoad, setReLoad] = useState(false);
    const [pageDetailItem, setPageDetailItem] = useState({});
    const [pageDetailVisible, setPageDetailVisible] = useState(false);
    const [objectDetailItem, setObjectDetailItem] = useState({
      schemeCode: 'agile_issue',
    });
    const value = {
      ...props,
      pageDetailItem,
      setPageDetailItem,
      pageDetailVisible,
      setPageDetailVisible,
      objectDetailItem,
      setObjectDetailItem,
      reLoad,
      setReLoad,
    };
    return (
      <PageConfigContext.Provider value={value}>
        {props.children}
      </PageConfigContext.Provider>
    );
  },
));
