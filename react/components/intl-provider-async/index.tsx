import React from 'react';
import {
  asyncLocaleProvider, stores,
} from '@choerodon/boot';
import { useCreation } from 'ahooks';

const { AppState } = stores;
interface IIntlProviderAsync {
  getMessage?: (language: string) => Promise<any>
}
const IntlProviderAsync: React.FC<IIntlProviderAsync> = ({ children, getMessage }) => {
  const language = AppState.currentLanguage;
  const Provider = useCreation(() => asyncLocaleProvider(language, () => (getMessage ? getMessage(language) : import(/* webpackInclude: /\index.(ts|js)$/ */ `../../locale/${language}`))), [language]);

  return (
    <Provider>
      {children}
    </Provider>
  );
};
IntlProviderAsync.defaultProps = {
  getMessage: undefined,
};
export default IntlProviderAsync;
