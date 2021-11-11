import React, {
  createContext, useMemo, useContext,
} from 'react';
import { inject } from 'mobx-react';
import { injectIntl } from 'react-intl';
import {
  DataSet,
} from 'choerodon-ui/pro';
import GanntDependencyModalDataSet from './GanntDependencyModalDataSet';
import { IModalProps } from '@/common/types';

export interface IGanttDependencyModalProps{
    onOk?:Function

}
interface Context extends IGanttDependencyModalProps {
    dataset: DataSet
    modal?: IModalProps
}
const Store = createContext({} as Context);

export default function useGanntDependencyModal() {
  return useContext(Store);
}
export const StoreProvider = inject('AppState')(injectIntl(
  (props) => {
    const { children } = props;
    const dataset = useMemo(() => new DataSet(GanntDependencyModalDataSet()), []);
    return (
      <Store.Provider value={{ ...props, dataset }}>
        {children}
      </Store.Provider>
    );
  },
));
