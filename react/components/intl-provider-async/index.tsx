import React, { memo } from 'react';
import {
  asyncLocaleProvider, stores,
} from '@choerodon/boot';

const { AppState } = stores;
interface IIntlProviderAsync {
  getMessage?: (language: string) => Promise<any>
  children?: any
}
const IntlProviderAsync: React.FC<IIntlProviderAsync> = ({ children, getMessage }) => {
  const language = AppState.currentLanguage;
  const Provider = asyncLocaleProvider(language, () => (getMessage ? getMessage(language) : import(/* webpackInclude: /\index.(ts|js)$/ */ `../../locale/${language}`)));

  return (
    <Provider>
      {children}
    </Provider>
  );
};
IntlProviderAsync.defaultProps = {
  getMessage: undefined,
  children: undefined,
};
export default memo(IntlProviderAsync);
