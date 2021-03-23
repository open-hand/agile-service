import React, { useCallback } from 'react';
import {
  Page, Header, Content, Breadcrumb,
} from '@choerodon/master';
import { Button } from 'choerodon-ui/pro';
import { DragDropContext } from 'react-beautiful-dnd';
import Columns from './components/columns';
import UnsetColumn from './components/unset-column';
import styles from './index.less';

const KanbanTemplateCreate = () => {
  const handleDragEnd = useCallback(() => {

  }, []);
  return (
    <Page>
      <Header>
        <Button
          icon="playlist_add"
        >
          创建列
        </Button>
      </Header>
      <Breadcrumb />
      <Content style={{
        borderTop: '1px solid rgb(216, 216, 216)',
      }}
      >
        <DragDropContext
          onDragEnd={handleDragEnd}
        >
          <div className={styles.container}>
            <div className={styles.columns_section}>
              <Columns />
            </div>
            <UnsetColumn className={styles.unset} />
          </div>
        </DragDropContext>
      </Content>
    </Page>
  );
};

export default KanbanTemplateCreate;
