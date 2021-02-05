import React, { useState, useCallback } from 'react';
import WYSIWYGEditor from '@/components/WYSIWYGEditor';
// @ts-ignore
import Delta from 'quill-delta';
import './AddComment.less';

interface Props {
  onSubmit: (delta: Delta) => Promise<any>
}
const Comments: React.FC<Props> = ({ onSubmit }) => {
  const [adding, setAdding] = useState(false);
  const [value, setValue] = useState<Delta>();
  const cancel = () => {
    setAdding(false);
    setValue(undefined);
  };
  const handleChange = useCallback((delta: Delta) => {
    setValue(delta);
  }, []);
  // 校验评论是否为空
  function verifyComment(comment: Delta) {
    let result = false;
    if (comment && comment.length) {
      comment.forEach((item:any) => {
        // @ts-ignore
        if (!result && item.insert && (item.insert.image || item.insert.trim())) {
          result = true;
        }
      });
    }
    return result;
  }

  const handleCreateCommit = async (delta: Delta) => {
    if (delta && verifyComment(delta)) {
      try {
        await onSubmit(delta);
        cancel();
      } catch (error) {
        //
      }
    } else {
      cancel();
    }
  };

  return adding ? (
    <div className="line-start mt-10 c7n-editIssue-addComment" style={{ width: '100%' }}>
      <WYSIWYGEditor
        autoFocus
        bottomBar
        style={{ height: 200, width: '100%' }}
        handleDelete={() => {
          cancel();
        }}
        value={value}
        onChange={handleChange}
        handleSave={handleCreateCommit}
      />
    </div>
  ) : (
    <div
      role="none"
      onClick={() => setAdding(true)}
      style={{
        background: 'rgba(0,0,0,0.03)',
        border: '1px solid rgba(0,0,0,0.20)',
        borderRadius: '5px',
        height: 36,
        lineHeight: '32px',
        width: '100%',
        color: 'rgba(0,0,0,0.65)',
        paddingLeft: 10,
        cursor: 'pointer',
      }}
    >
      点击添加评论…
    </div>
  );
};

export default Comments;
