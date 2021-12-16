import React, { memo, useMemo } from 'react';
import C7NLocaleProvider from '@choerodon/master/lib/components/c7n-locale-provider';
import { usePersistFn } from 'ahooks';

type C7NLocaleProviderProps = React.ComponentProps<typeof C7NLocaleProvider>;
type IIntlProviderAsyncExtendProps = Omit<C7NLocaleProviderProps, 'importer'> & Partial<Pick<C7NLocaleProviderProps, 'importer'>>
interface IIntlProviderAsync extends IIntlProviderAsyncExtendProps {
  importer?: IIntlProviderAsyncExtendProps['importer']
  /**
   * @deprecated
   * 改用 importer
   */
  getMessage?: (language: string) => Promise<any>
  // children?: any
}

const IntlProviderAsync: React.FC<IIntlProviderAsync> = ({
  children, getMessage, importer: propsImporter, ...otherProps
}) => {
  const importer = usePersistFn(propsImporter || getMessage || ((language: string) => import(/* webpackInclude: /\index.(ts|js)$/ */ `../../locale/${language}`)));
  return (
    <C7NLocaleProvider importer={importer} {...otherProps}>
      {children}
    </C7NLocaleProvider>
  );
};
IntlProviderAsync.defaultProps = {
  getMessage: undefined,
  importer: (language: string) => import(/* webpackInclude: /\index.(ts|js)$/ */ `../../locale/${language}`),
};
export default IntlProviderAsync;
