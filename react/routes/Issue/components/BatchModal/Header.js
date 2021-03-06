import React from 'react';
import { Button } from 'choerodon-ui/pro';

function Header({
  selected, close, onClickEdit, onClickDelete, hasBatchDeletePermission,
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
                className="c7n-batch-header-btn"
                icon="edit-o"
                style={{ color: 'white', marginRight: 6 }}
                onClick={onClickEdit}
              >
                批量编辑
              </Button>
              <Button
                className="c7n-batch-header-btn"
                icon="delete_sweep-o"
                style={{ color: 'white', marginRight: 18 }}
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

export default Header;
