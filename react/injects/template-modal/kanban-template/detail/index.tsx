import React, { useEffect } from 'react';
import { useCreation } from 'ahooks';
import { observer } from 'mobx-react-lite';
import { Loading } from '@/components';
import Columns from './components/columns';
import styles from './index.less';
import KanbanTemplateDetailStore from './store';

export interface KanbanTemplateDetailContext {
  store: KanbanTemplateDetailStore
}
interface KanbanTemplateDetailProps {
  templateId: string
}
export const Context = React.createContext<KanbanTemplateDetailContext>({} as KanbanTemplateDetailContext);
const KanbanTemplateDetail: React.FC<KanbanTemplateDetailProps> = ({ templateId }) => {
  const store = useCreation(() => new KanbanTemplateDetailStore(templateId), [templateId]);
  useEffect(() => {
    store.refresh();
  }, [store]);
  return (
    <>
      <Loading loading={store.loading} />
      <Context.Provider value={{ store }}>
        <div className={styles.container}>
          <div className={styles.columns_section}>
            <Columns columns={store.columns} />
          </div>
        </div>
      </Context.Provider>
    </>
  );
};

export default observer(KanbanTemplateDetail);
