import React, { useCallback, useEffect } from 'react';
import {
  Page, Header, Content, Breadcrumb, HeaderButtons,
} from '@choerodon/master';
import { DragDropContext, DropResult } from 'react-beautiful-dnd';
import { useParams } from 'react-router';
import { useCreation } from 'ahooks';
import { observer } from 'mobx-react-lite';
import { Loading } from '@/components';
import Columns from './components/columns';
import UnsetColumn from './components/unset-column';
import styles from './index.less';
import KanbanTemplateDetailStore from './store';
import openKanbanTemplateColumnModal from './components/modal';

export interface KanbanTemplateDetailContext {
  store: KanbanTemplateDetailStore
}
export const Context = React.createContext<KanbanTemplateDetailContext>({} as KanbanTemplateDetailContext);
const KanbanTemplateDetail = () => {
  const { templateId } = useParams<any>();
  const store = useCreation(() => new KanbanTemplateDetailStore(templateId), [templateId]);
  useEffect(() => {
    store.refresh();
  }, [store]);
  const handleDragEnd = useCallback((result: DropResult) => {
    if (result.destination) {
      if (result.destination.droppableId === 'columndrop') {
        store.moveColumn(result);
      } else {
        // 移动状态
        store.moveStatus(result);
      }
    }
  }, [store]);
  const handleColumnCreateClick = useCallback(() => {
    openKanbanTemplateColumnModal({
      boardId: templateId,
      onSubmit: () => {
        store.refresh();
      },
    });
  }, [store, templateId]);
  return (
    <Page>
      <Header>
        <HeaderButtons
          showClassName={false}
          items={[
            {
              name: '创建列',
              icon: 'playlist_add',
              display: true,
              handler: handleColumnCreateClick,
            },
          ]}
        />
      </Header>
      <Breadcrumb title="修改看板模板" />
      <Content style={{
        borderTop: '1px solid var(--divider)',
        display: 'flex',
      }}
      >
        <Loading loading={store.loading} />
        <DragDropContext
          onDragEnd={handleDragEnd}
        >
          <Context.Provider value={{ store }}>
            <div className={styles.container}>
              <div className={styles.columns_section}>
                <Columns columns={store.columns} />
              </div>
              <UnsetColumn className={styles.unset} />
            </div>
          </Context.Provider>
        </DragDropContext>
      </Content>
    </Page>
  );
};

export default observer(KanbanTemplateDetail);
