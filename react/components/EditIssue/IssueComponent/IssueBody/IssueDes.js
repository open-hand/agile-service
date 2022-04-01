import React, {
  useState, useEffect, useContext,
} from 'react';
import {
  Icon, Button, Tooltip,
} from 'choerodon-ui/pro';
import { Choerodon } from '@choerodon/master';
import WYSIWYGViewer from '@/components/CKEditorViewer';
import WYSIWYGEditor from '@/components/CKEditor';
import { useDetailContainerContext } from '@/components/detail-container/context';
import { issueApi } from '@/api';
import EditIssueContext from '../../stores';
import Divider from './Divider';

const IssueDes = ({ reloadIssue, setIssueLoading }) => {
  const { setDescriptionChanged } = useDetailContainerContext();
  const [editDesShow, setEditDesShow] = useState(false);
  const [editDes, setEditDes] = useState('');
  const { store, disabled, descriptionEditRef } = useContext(EditIssueContext);
  const { description, descriptionTemplate, typeCode } = store.getIssue;
  useEffect(() => {
    setEditDes(description);
    setEditDesShow(false);
  }, [description]);

  const updateIssueDes = async (value) => {
    const { issueId, objectVersionNumber } = store.getIssue;

    const newValue = value || editDes;
    try {
      const text = newValue;
      const obj = {
        issueId,
        objectVersionNumber,
        description: text,
      };
      await store.update(obj);
      setDescriptionChanged(false);
      setEditDesShow(false);
      if (reloadIssue) {
        reloadIssue(issueId);
      }
    } catch (error) {
      Choerodon.prompt(error.message);
    }
  };
  useEffect(() => {
    descriptionEditRef.current = editDesShow;
  }, [descriptionEditRef, editDesShow]);
  const renderDes = () => {
    if (editDesShow === undefined) {
      return null;
    }
    if (!description || editDesShow) {
      return (
        editDesShow && (
          <div
            className="line-start mt-10"
          >
            <WYSIWYGEditor
              autoFocus
              footer
              value={editDes ?? descriptionTemplate}
              style={{
                height: 'auto', width: '100%', minHeight: 300,
              }}
              onChange={(value) => {
                setEditDes(value);
                setDescriptionChanged(value !== description);
              }}
              onCancel={() => {
                setEditDesShow(false);
                setEditDes(description);
              }}
              onOk={updateIssueDes}
            />
          </div>
        )
      );
    }
    return (
      <div className="c7n-content-wrapper">
        <div
          className="mt-10 c7n-description"
          role="none"
        >
          <WYSIWYGViewer value={description} />
        </div>
      </div>
    );
  };

  const callback = (value) => {
    updateIssueDes(value);
  };

  return (
    <div id="des">
      <Divider />
      <div className="c7n-title-wrapper">
        <div className="c7n-title-left">
          {/* <Icon type="subject c7n-icon-title" /> */}
          <span>描述</span>
        </div>
        {/* <div style={{
          flex: 1, height: 1, borderTop: '1px solid rgba(0, 0, 0, 0.08)', marginLeft: '14px',
        }}
        /> */}
        {!disabled && (
          <div className="c7n-title-right" style={{ marginLeft: '14px', position: 'relative' }}>
            <Tooltip placement="topRight" autoAdjustOverflow={false} title="修改">
              <Button
                style={{ padding: '0 6px' }}
                onClick={() => {
                  setEditDesShow(true);
                  setEditDes(description);
                }}
                disabled={editDesShow}
              >
                <Icon
                  className="c7n-des-fullEdit"
                  role="none"
                  type="edit-o icon"
                />
              </Button>
            </Tooltip>
          </div>
        )}
      </div>
      {renderDes()}
    </div>
  );
};

export default IssueDes;
