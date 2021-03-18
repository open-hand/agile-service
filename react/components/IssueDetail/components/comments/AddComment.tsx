import React, { useState, useCallback } from 'react';
import WYSIWYGEditor from '@/components/CKEditor';

interface Props {
  onSubmit: (delta: string) => Promise<any>
}
const Comments: React.FC<Props> = ({ onSubmit }) => {
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
      } finally {
        cancel();
      }
    } else {
      cancel();
    }
  };

  return adding ? (
    <div className="line-start mt-10" style={{ width: '100%' }}>
      <WYSIWYGEditor
        autoFocus
        footer
        style={{ height: 200, width: '100%' }}
        onCancel={() => {
          cancel();
        }}
        value={value}
        onChange={handleChange}
        onOk={handleCreateCommit}
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
        marginBottom: 10,
      }}
    >
      点击添加评论…
    </div>
  );
};

export default Comments;
