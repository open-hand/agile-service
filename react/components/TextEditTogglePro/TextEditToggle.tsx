import React, {
  useState, useRef, cloneElement, useEffect,
} from 'react';
import classNames from 'classnames';
import styles from './TextEditToggle.less';

interface Props {
  disabled?: boolean
  children: JSX.Element
  onSubmit: (data: any) => void
  initValue: any
  renderText: (text: any) => JSX.Element
}

const TextEditToggle: React.FC<Props> = ({
  disabled, children: editor, onSubmit, initValue, renderText,
}) => {
  const [editing, setEditing] = useState(false);
  const dataRef = useRef(initValue);
  const editorRef = useRef(null);
  const containerRef = useRef<HTMLDivElement>(null);
  useEffect(() => {
    dataRef.current = initValue;
  }, [initValue]);
  useEffect(() => {
    if (editing && editorRef.current) {
      // console.log('focus', editorRef.current);
      // @ts-ignore
      // editorRef.current.focus();
    }
  });
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
    const editorProps: any = {
      // tabIndex: -1,
      defaultValue: initValue,
      onChange: handleChange,
      onBlur: handleEditorBlur,
      ref: editorRef,
      autoFocus: true,
    };
    if (containerRef.current) {
      editorProps.style = {
        width: containerRef.current.getBoundingClientRect().width,
        height: containerRef.current.getBoundingClientRect().height,
      };
    }
    return editing ? cloneElement(editor, editorProps) : <div className={styles.text}>{renderText(dataRef.current)}</div>;
  };
  const handleFocus = () => {
    if (!disabled) {
      showEditor();
    }
  };
  return (
    <div
      ref={containerRef}
      className={classNames(
        styles.container,
        { [styles.disabled]: disabled },
      )}
      // eslint-disable-next-line jsx-a11y/no-noninteractive-tabindex
      tabIndex={0}
      onFocus={handleFocus}
    >
      {getCellRenderer()}
    </div>
  );
};

export default TextEditToggle;
