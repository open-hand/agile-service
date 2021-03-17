import React, {
  useRef, useMemo, useState, useCallback, useImperativeHandle, useEffect,
} from 'react';
import generateTask from '@/routes/project-report/report-preview/generateTask';
import html2canvas from 'html2canvas';
import PreviewReport from '@/routes/project-report/report-preview/Preview';
import ReactDOM from 'react-dom';

export interface IExportProps {
  export: (callback: (canvas: HTMLCanvasElement[]) => void) => void
}
interface Props {
  innerRef: React.Ref<IExportProps>
}

const Export: React.FC<Props> = ({ innerRef }) => {
  const containerRef = useRef<HTMLDivElement>(null);
  const callbackRef = useRef<(canvas: HTMLCanvasElement[]) => void>();
  const task = useMemo(() => generateTask('export', async () => {
    if (containerRef.current) {
      const container = containerRef.current;
      const blocks = container.getElementsByClassName('c7n-project-report-block');
      const elements = Array.from(blocks);
      const canvases = [];
      for (let i = 0; i < elements.length; i += 1) {
        const element = elements[i];
        // eslint-disable-next-line no-await-in-loop
        const canvas = await html2canvas(element as HTMLElement, {
          allowTaint: true,
          useCORS: true,
          logging: false,
          height: element.scrollHeight,
          width: container.scrollWidth,
          windowHeight: element.scrollHeight,
          windowWidth: container.scrollWidth,
        });
        canvases.push(canvas);
      }
      setExporting(false);
      task.reset();
      callbackRef.current && callbackRef.current(canvases);
    }
  }), []);
  const [exporting, setExporting] = useState(false);
  const handleExport = useCallback((callback) => {
    setExporting(true);
    callbackRef.current = callback;
  }, []);
  useImperativeHandle(innerRef, () => ({
    export: handleExport,
  }));
  return exporting ? (
    ReactDOM.createPortal(
      <div
        style={{
          position: 'fixed', top: -100000, left: -100000, width: '100%',
        }}
      >
        <PreviewReport
          task={task}
          innerRef={containerRef}
          style={{ padding: '20px 10px' }}
          scale={1.5}
        />
      </div>,
      document.body,
    )
  ) : null;
};

export default Export;
