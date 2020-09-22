import React, {
  useRef, useMemo, useState, useCallback, useImperativeHandle,
} from 'react';
import generateTask from '@/routes/project-report/report-preview/generateTask';
import html2canvas from 'html2canvas';
import PreviewReport from '@/routes/project-report/report-preview';
import ReactDOM from 'react-dom';

export interface IExportProps {
  export: () => void
}
interface Props {
  innerRef: React.Ref<IExportProps>
  onExport: (canvas: HTMLCanvasElement) => void
}

const Export: React.FC<Props> = ({ innerRef, onExport }) => {
  const containerRef = useRef<HTMLDivElement>(null);
  const task = useMemo(() => generateTask('export', () => {
    if (containerRef.current) {
      const element = containerRef.current;
      html2canvas(element, {
        allowTaint: true,
        useCORS: true,
        logging: false,
        height: element.scrollHeight,
        width: element.scrollWidth,
        windowHeight: element.scrollHeight,
        windowWidth: element.scrollWidth,
      }).then((canvas) => {
        setExporting(false);
        task.reset();
        onExport(canvas);
      });
    }
  }), [onExport]);
  const [exporting, setExporting] = useState(false);
  const handleExport = useCallback(() => {
    setExporting(true);
  }, []);
  useImperativeHandle(innerRef, () => ({
    export: handleExport,
  }));
  return exporting ? (
    ReactDOM.createPortal(
      <div style={{
        position: 'fixed', top: -100000, left: -100000, width: '100%',
      }}
      >
        <PreviewReport task={task} innerRef={containerRef} />
      </div>,
      document.body,
    )
  ) : null;
};

export default Export;
