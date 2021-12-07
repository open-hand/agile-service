import React from 'react';
import { Button } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import classNames from 'classnames';

function Header({
  selected, close, onClickEdit, onClickMove, onClickDelete, hasBatchDeletePermission, issueSearchStore,
}) {
  return (
    <>
      <div style={{ display: 'flex', alignItems: 'center' }}>
        <span style={{
          fontSize: '30px', fontWeight: 500, marginRight: 12, color: '#FFFFFF',
        }}
        >
          {`${selected.length}`}
        </span>
        <span style={{ fontSize: '16px', color: 'rgba(255, 255, 255, 0.8)', marginTop: 5 }}>项已选中</span>
      </div>
      <div style={{
        marginLeft: 'auto', height: 56, display: 'flex', alignItems: 'center',
      }}
      >
        {
          hasBatchDeletePermission && (
            <div style={{
              display: 'inline-block', height: 56, lineHeight: '56px', borderRight: '1px solid #95A5FF',
            }}
            >
              <Button
                className={classNames('c7n-batch-header-btn', {
                  'c7n-batch-header-currentActionBtn': issueSearchStore.batchAction === 'edit',
                })}
                icon="edit-o"
                onClick={onClickEdit}
              >
                批量编辑
              </Button>
              <Button
                className={classNames('c7n-batch-header-btn', {
                  'c7n-batch-header-currentActionBtn': issueSearchStore.batchAction === 'move',
                })}
                icon="drive_file_move-o"
                onClick={onClickMove}
              >
                批量移动
              </Button>
              <Button
                className={classNames('c7n-batch-header-btn', {
                  'c7n-batch-header-currentActionBtn': issueSearchStore.batchAction === 'delete',
                })}
                icon="delete_sweep-o"
                onClick={onClickDelete}
              >
                批量删除
              </Button>
            </div>
          )
        }
        <Button
          icon="close"
          shape="circle"
          style={{ color: 'white', marginRight: -10, marginLeft: 10 }}
          onClick={close}
        />
      </div>
    </>
  );
}

export default observer(Header);
