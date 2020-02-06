import React, { createContext } from 'react';
import { inject } from 'mobx-react';
import { DataSet } from 'choerodon-ui/pro';
import CreateFormDataSet from './FormDataSet';
import useStore from './useStore';

const Store = createContext();
export default Store;

export const StoreProvider = inject('AppState')(
  (props) => {
    const {
      formatMessage, AppState: { currentMenuType: { type, id, organizationId } }, schemeCode, record,
    } = props;
    const isEdit = !!record;
    const store = useStore(type, id, organizationId);
    const createFormDataSet = new DataSet(CreateFormDataSet({
      formatMessage, type, store, schemeCode, id, organizationId, isEdit,
    }));

    if (isEdit) {
      createFormDataSet.transport.read = () => ({
        url: `/agile/v1/${type}s/${id}/object_scheme_field/${record.get('id')}?organizationId=${organizationId}`,
        method: 'get',
      });
      createFormDataSet.query();
    }
    
    const value = {
      ...props,
      isEdit,
      createFormDataSet,
    };

    return (
      <Store.Provider value={value}>
        {props.children}
      </Store.Provider>
    );
  },
);
