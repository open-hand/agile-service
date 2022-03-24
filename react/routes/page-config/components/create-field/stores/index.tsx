import React, { createContext, useEffect, useMemo } from 'react';
import { inject } from 'mobx-react';
import { DataSet } from 'choerodon-ui/pro';
import moment from 'moment';
import { sortBy } from 'lodash';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import type { IntlShape } from 'react-intl';
import { useComputed, observer, useObservable } from 'mobx-react-lite';
import { AppStateProps, IModalProps } from '@/common/types';
import FormDataSet from './FormDataSet';
import useStore, { Store as HookStore } from './useStore';
import { IFieldPostDataProps } from '../CreateField';
import ContextOptionsDataSet from './ContextOptionsDataSet';
import useFormatMessage from '@/hooks/useFormatMessage';

interface Context {
  formatMessage: IntlShape['formatMessage'],
  AppState: AppStateProps,
  isEdit?: boolean,
  store: HookStore,
  defaultContext?: Array<{ code: string, disabled?: boolean } | string>,
  onSubmitLocal?: (data: IFieldPostDataProps) => Promise<boolean> | boolean,
  localCheckCode?: (code: string) => Promise<boolean> | boolean,
  localCheckName?: (name: string) => Promise<boolean> | boolean,
  schemeCode: string,
  record?: Record,
  formDataSet: DataSet,
  handleRefresh?: () => void,
  modal: IModalProps,
}
const Store = createContext({} as Context);
export type CreateFiledProps = Pick<Context, 'isEdit' | 'onSubmitLocal' |
  'localCheckCode' | 'localCheckName' | 'schemeCode' | 'handleRefresh' | 'record'>;
export default Store;
export const StoreProvider: React.FC<Context> = inject('AppState')(observer(
  (props) => {
    const {
      AppState: { currentMenuType: { type, id, organizationId } },
      schemeCode, record, localCheckCode, localCheckName, defaultContext: propsDefaultContext,
    } = props;
    const formatMessage = useFormatMessage();
    const isEdit = !!record;
    const store = useStore(type, id, organizationId);
    const contextOptionsDataSet = useMemo(() => new DataSet(ContextOptionsDataSet({ isEdit, store, oldRecord: record })), [isEdit, record, store]);
    const defaultContext: string[] | undefined = useMemo(() => propsDefaultContext?.map((item: any) => (typeof (item) === 'string' ? item : item.code)), []);
    const formDataSet = useMemo(() => new DataSet(FormDataSet({
      formatMessage,
      type,
      store,
      schemeCode,
      id,
      isEdit,
      oldRecord: record,
      localCheckCode,
      localCheckName,
      defaultContext,
      contextOptionsDataSet,
    })), [contextOptionsDataSet, defaultContext, formatMessage, id, isEdit, localCheckCode, localCheckName, record, schemeCode, store, type]);

    useEffect(() => {
      if (!isEdit && contextOptionsDataSet.length > 0 && defaultContext) {
        formDataSet.current?.set('context', defaultContext);
      }
    }, [contextOptionsDataSet.length, defaultContext, formDataSet, isEdit]);

    useEffect(() => {
      if (isEdit) {
        formDataSet.transport.read = () => ({
          url: `/agile/v1/${type}s/${id}/object_scheme_field/${record?.get('id')}?organizationId=${organizationId}`,
          method: 'get',
          transformResponse: (response) => {
            try {
              const data = JSON.parse(response);
              if (data.defaultValue === '') {
                data.defaultValue = undefined;
              }
              data.issueTypeVOList = sortBy(data.issueTypeVOList, (i) => (i.enabled ? 1 : -1));
              return data;
            } catch (error) {
              return response;
            }
          },
        });
        formDataSet.query().then((data) => {
          const dateList = ['date', 'datetime', 'time'];
          const multipleList = ['checkbox', 'multiple', 'multiMember'];
          const dateFormat = ['YYYY-MM-DD', 'YYYY-MM-DD HH:mm:ss', 'HH:mm:ss'];
          const dateIndex = dateList.indexOf(data.fieldType);
          if (dateIndex !== -1) {
            // 格式化日期类型
            formDataSet.current?.set('defaultValue', data.defaultValue && data.defaultValue !== '' ? moment(data.defaultValue, 'YYYY-MM-DD HH:mm:ss').format(dateFormat[dateIndex]) : undefined);
            // 变换数据，从extraConfig -> check
            formDataSet.current?.set('check', data.extraConfig);
          }
          if (data.fieldType === 'number') {
            formDataSet.current?.set('check', data.extraConfig);
          }
          if (data.issueTypeVOList && Array.isArray(data.issueTypeVOList)) {
            formDataSet.current?.set('context', data.issueTypeVOList.map((item: any) => item.id));
          }
          if (multipleList.indexOf(data.fieldType) !== -1) {
            formDataSet.current?.set('defaultValue', data.defaultValue && data.defaultValue.split && data.defaultValue.split(','));
          }
        });
      }
    }, [formDataSet]);

    const value = {
      ...props,
      isEdit,
      store,
      formatMessage: formatMessage as any,
      formDataSet,
    };

    return (
      <Store.Provider value={value}>
        {props.children}
      </Store.Provider>
    );
  },
));
