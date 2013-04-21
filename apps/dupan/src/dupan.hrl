-record(file, {id,                              % fs_id
               path,
               name,
               mtime,
               ctime,
               size,
               md5,
               dlink}).

-record(dir, {id,                              % fs_id
              path,
              name,
              mtime,
              ctime}).
