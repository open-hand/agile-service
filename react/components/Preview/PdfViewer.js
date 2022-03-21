import React, { useCallback, useEffect, useState } from 'react';

const Pdf = ({ file }) => {
  const [newUrl, setNewUrl] = useState();
  const loadNewUrl = useCallback(() => {
    fetch(file)
      .then((res) => res?.blob())
      .then((blob) => {
        blob && setNewUrl(URL.createObjectURL(blob));
      });
  }, [file]);

  useEffect(() => {
    loadNewUrl();
  }, [loadNewUrl]);

  if (!newUrl) {
    return <></>;
  }

  return (
    <embed
      src={newUrl}
      style={{
        width: '100%',
        height: '100%',
      }}
    />
  );
};

export default Pdf;
