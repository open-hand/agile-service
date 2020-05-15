import React, {
  useState, useEffect, useContext,
} from 'react';
import {
  Icon, Button, Tooltip,  
} from 'choerodon-ui';

import WYSIWYGViewer from '@/components/WYSIWYGViewer';
import WYSIWYGEditor from '@/components/WYSIWYGEditor';
import { text2Delta, returnBeforeTextUpload } from '@/utils/richText';
import FullEditor from '../../../FullEditor';
import { updateIssue } from '../../../../api/NewIssueApi';
import EditIssueContext from '../../stores';
import Divider from './Divider';

const bugDefaultDes = [{ attributes: { bold: true }, insert: '步骤' }, { insert: '\n' }, { attributes: { list: 'ordered' }, insert: '\n\n\n' }, { attributes: { bold: true }, insert: '结果' }, { insert: '\n\n' }, { attributes: { bold: true }, insert: '期望' }, { insert: '\n' }];
const IssueDes = ({ reloadIssue }) => {
  const [editDesShow, setEditDesShow] = useState(false);
  const [fullEdit, setFullEdit] = useState(false);
  const [editDes, setEditDes] = useState('');
  const { store, disabled } = useContext(EditIssueContext);
  const { description, typeCode } = store.getIssue;
  useEffect(() => {    
    setEditDes(description);
    setEditDesShow(false);
  }, [description]);

  // 校验描述是否为空
  const verifyDes = (delta) => {
    let result = false;
    if (delta && delta.length) {
      delta.forEach((item) => {
        if (!result && item.insert && (item.insert.image || item.insert.trim())) {
          result = true;
        }
      });
    }
    return result;
  };

  const updateIssueDes = (value) => {
    // if (verifyDes(value || editDes)) {
    const { issueId, objectVersionNumber } = store.getIssue;
    const obj = {
      issueId,
      objectVersionNumber,
    };
    const newValue = value || editDes;
    if (newValue) {
      returnBeforeTextUpload(newValue, obj, updateIssue, 'description')
        .then(() => {
          if (reloadIssue) {
            reloadIssue(issueId);
          }
        });
    }
    setEditDesShow(false);
    setFullEdit(false);
    // }
  };

  const renderDes = () => {
    if (editDesShow === undefined) {
      return null;
    }
    if (!description || editDesShow) {
      return (
        editDesShow && (
          <div
            className="line-start mt-10 two-to-one"
          >
            <div style={{
              width: '100%',
              position: 'absolute',
              top: 0,
              bottom: 0,         
            }}
            >
              <WYSIWYGEditor
                autoFocus
                bottomBar
                value={typeCode === 'bug' ? text2Delta(editDes) || bugDefaultDes : text2Delta(editDes)}
                style={{
                  height: '100%', width: '100%',
                }}
                onChange={(value) => {
                  setEditDes(value);                 
                }}
                handleDelete={() => {
                  setEditDesShow(false);
                  setEditDes(description);                  
                }}
                handleSave={() => {
                  setEditDesShow(false);                             
                  updateIssueDes();
                }}
              />
            </div>
          </div>
        )
      );
    } else {
      return (
        <div className="c7n-content-wrapper">
          <div
            className="mt-10 c7n-description"
            role="none"
          >
            <WYSIWYGViewer data={description} />
          </div>
        </div>
      );
    }
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
            <Tooltip title="全屏编辑" getPopupContainer={triggerNode => triggerNode.parentNode}>
              <Button style={{ padding: '0 6px' }} className="leftBtn" funcType="flat" onClick={() => setFullEdit(true)}>
                <Icon type="zoom_out_map icon" style={{ marginRight: 2 }} />
              </Button>
            </Tooltip>
            <Tooltip placement="topRight" autoAdjustOverflow={false} title="编辑">
              <Button
                style={{ padding: '0 6px' }}
                className="leftBtn"
                funcType="flat"
                onClick={() => {
                  setEditDesShow(true);
                  setEditDes(description);
                }}
              >
                <Icon
                  className="c7n-des-fullEdit"
                  role="none"
                  type="mode_edit icon"
                />
              </Button>
            </Tooltip>
          </div>
        )}
      </div>
      {renderDes()}
      {
        fullEdit ? (
          <FullEditor
            autoFocus
            initValue={text2Delta(editDes)}
            visible={fullEdit}
            onCancel={() => setFullEdit(false)}
            onOk={callback}
          />
        ) : null
      }
    </div>
  );
};

export default IssueDes;
