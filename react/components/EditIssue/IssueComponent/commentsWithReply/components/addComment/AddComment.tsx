import React, { useState, useCallback, useImperativeHandle } from 'react';
import WYSIWYGEditor from '@/components/CKEditor';
import './AddComment.less';

interface Props {
  onSubmit: (data: string) => Promise<any>
  addingRef: React.MutableRefObject<{
    adding: boolean,
    setAdding: (adding: boolean) => void
    setAddValue: (v: string) => void
  } | null>
  editingRef: React.MutableRefObject<{
    editing: boolean,
    setEditing: (editing: boolean) => void
    setEditValue: (v: string) => void
    initValue: string
  } | null>
  replyingRef: React.MutableRefObject<{
    replying: boolean,
    setReplying:(replying: boolean) => void
    setReplyValue: (v: string) => void
      } | null>
  commentsHeight: number | undefined
  projectId?: string,
}
const Comments: React.FC<Props> = ({
  onSubmit, addingRef, editingRef, replyingRef, commentsHeight, projectId,
}) => {
  const [adding, setAdding] = useState(false);
  const [value, setValue] = useState<string>('');
  const cancel = () => {
    setAdding(false);
    setValue('');
  };
  const handleChange = useCallback((delta: string) => {
    setValue(delta);
  }, []);
  // 校验评论是否为空
  function verifyComment(comment: string) {
    return comment.length > 0;
  }

  const handleCreateCommit = async (delta: string) => {
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

  const handleAdding = useCallback(() => {
    setAdding(true);
    if (editingRef?.current?.editing) {
      editingRef?.current?.setEditValue(editingRef?.current?.initValue);
      editingRef?.current?.setEditing(false);
    }
    if (replyingRef?.current?.replying) {
      replyingRef?.current?.setReplyValue('');
      replyingRef?.current?.setReplying(false);
    }
  }, [editingRef, replyingRef]);

  useImperativeHandle(addingRef, () => ({
    adding,
    setAdding,
    setAddValue: setValue,
  }));

  return adding ? (
    <div className="line-start mt-10 c7n-editIssue-addComment" style={{ width: '100%', overflowX: 'hidden' }}>
      <WYSIWYGEditor
        autoFocus
        footer
        style={{ minHeight: 300, maxHeight: (commentsHeight || 500) - 70, width: '100%' }}
        onCancel={() => {
          cancel();
        }}
        value={value}
        onChange={handleChange}
        onOk={handleCreateCommit}
        projectId={projectId}
      />
    </div>
  ) : (
    <div
      role="none"
      onClick={handleAdding}
      style={{
        background: 'rgba(158,173,190,0.04)',
        border: '1px solid var(--divider)',
        color: 'var(--text-color)',
        borderRadius: '5px',
        height: 36,
        lineHeight: '32px',
        width: '100%',
        paddingLeft: 10,
        cursor: 'pointer',
      }}
    >
      点击添加评论…
    </div>
  );
};

export default Comments;
