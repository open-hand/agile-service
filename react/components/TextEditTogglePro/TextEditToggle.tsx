import React, {
  useState, useRef, cloneElement, useEffect,
} from 'react';
import { Output } from 'choerodon-ui/pro';

interface Props {
  children: JSX.Element
  onSubmit: (data: any) => void
  initValue: any
  renderText: (text: any) => JSX.Element
}

const TextEditToggle: React.FC<Props> = ({
  children: editor, onSubmit, initValue, renderText,
}) => {
  const [editing, setEditing] = useState(false);
  const dataRef = useRef(initValue);
  useEffect(() => {
    dataRef.current = initValue;
  }, [initValue]);
  const hideEditor = () => {
    if (editing) {
      setEditing(false);
    }
  };
  const showEditor = () => {
    if (!editing) {
      setEditing(true);
    }
  };
  const handleChange = (value: any) => {
    dataRef.current = value;
  };
  const handleEditorBlur = async () => {
    hideEditor();
    if (dataRef.current !== initValue) {
      await onSubmit(dataRef.current);
    }
  };
  const getCellRenderer = () => {
    const editorProps = {
      defaultValue: initValue,
      onChange: handleChange,
      onBlur: handleEditorBlur,
      autoFocus: true,
    };
    return editing ? cloneElement(editor, editorProps) : renderText(dataRef.current);
  };
  const handleFocus = () => {
    showEditor();
  };
  return (
    <Output
      tabIndex={0}
      renderer={getCellRenderer}
      onFocus={handleFocus}
    />
  );
};

export default TextEditToggle;
